import * as React from "react";

interface BlogCardProps {
  image: string;
  category: string;
  title: string;
  description: string;
  imageAspectRatio?: string;
}

export function BlogCard({
  image,
  category,
  title,
  description,
  imageAspectRatio = "aspect-[1.4]",
}: BlogCardProps) {
  return (
    <article className="flex-1 shrink px-6 py-7 bg-white shadow-lg basis-0 min-h-[580px] min-w-60 max-md:px-5">
      <img
        src={image}
        className={`object-contain w-full ${imageAspectRatio}`}
        alt={title}
      />
      <div className="flex-1 mt-8 w-full">
        <div className="w-full">
          <div className="text-sm font-semibold leading-none text-violet-700">
            {category}
          </div>
          <div className="mt-3 w-full">
            <div className="flex gap-4 items-start w-full">
              <h3 className="flex-1 shrink text-2xl font-semibold leading-none text-gray-900 basis-0">
                {title}
              </h3>
              <div className="pt-1 w-6">
                <img
                  src="https://api.builder.io/api/v1/image/assets/TEMP/6b24f54ba9367c982bcf2abc4abf9040b9311798?placeholderIfAbsent=true&apiKey=c00da45f4ed64d9893425a2094d3e1af"
                  className="object-contain w-6 aspect-square"
                  alt="Arrow icon"
                />
              </div>
            </div>
            <p className="mt-3 text-base leading-6 text-gray-500">
              {description}
            </p>
          </div>
        </div>
      </div>
    </article>
  );
}
